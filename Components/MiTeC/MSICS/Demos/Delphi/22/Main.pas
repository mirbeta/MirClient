unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  MSI_Processes, MiTeC_NativeDefs, MiTeC_Windows, CheckLst;

type
  TwndMain = class(TForm)
    ButtonPanel: TPanel;
    bOK: TButton;
    bRefresh: TButton;
    List: TListView;
    Label1: TLabel;
    clb: TCheckListBox;
    procedure FormCreate(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bRefreshClick(Sender: TObject);
  private
    FPL: TMiTeC_ProcessList;
    function GetTypes: TSystemHandleTypes;
  protected
  public
    procedure RefreshData;
  end;

var
  wndMain: TwndMain;

implementation

uses MiTeC_Routines;

{$R *.dfm}

procedure TwndMain.bOKClick(Sender: TObject);
begin
  Close;
end;

procedure TwndMain.bRefreshClick(Sender: TObject);
begin
  RefreshData;
end;

procedure TwndMain.FormCreate(Sender: TObject);
begin
  EnablePrivilege(SE_DEBUG_NAME);

  clb.Checked[Integer(OB_TYPE_FILE)-1]:=True;
  FPL:=TMiTeC_ProcessList.Create(Self);
  FPL.DetectionRange:=[dtHandles];
end;

function TwndMain.GetTypes: TSystemHandleTypes;
var
  i: Integer;
begin
  Result:=[];
  for i:=0 to clb.Items.Count-1 do
    if clb.Checked[i] then
      Result:=Result+[TSystemHandleType(i+1)];
end;

procedure TwndMain.RefreshData;
var
  i,c: Integer;
  p: THandleRecord;
begin
  List.Hide;
  ButtonPanel.Hide;
  Update;
  Screen.Cursor:=crHourglass;
  try
    FPL.HandleTypes:=GetTypes;
    FPL.RefreshData;
    List.Items.BeginUpdate;
    try
      List.Items.Clear;
      c:=FPL.HandleCount;
      for i:=0 to c-1 do begin
        p:=FPL.Handles[i];
        with List.Items.Add do begin
          Caption:=IntToStr(p.PID);
          SubItems.Add(p._ProcessName);
          SubItems.Add(Format('0x%x',[p.Handle]));
          SubItems.Add(p.TypeName);
          SubItems.Add(p.Name);
        end;
      end;
    finally
      List.Items.EndUpdate;
    end;
    ButtonPanel.Show;
    List.Show;
    Caption:=Format('Handles [%d]',[List.Items.Count]);
  finally
    Screen.Cursor:=crDefault;
  end;
end;

end.
